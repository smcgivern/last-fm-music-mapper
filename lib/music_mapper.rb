require './lib/last_fm'
require './lib/music_mapper/countries'
require 'rvg/rvg'

module MusicMapper
  USERS = {}
  ARTISTS = {}

  PERIODS = [
             {:identifier => '7day', :name => 'Last 7 days'},
             {:identifier => '1month', :name => 'Last month'},
             {:identifier => '3month', :name => 'Last 3 months'},
             {:identifier => '6month', :name => 'Last 6 months'},
             {:identifier => '12month', :name => 'Last 12 months'},
             {:identifier => 'overall', :name => 'Overall'},
            ]

  def self.thousands(s); s.to_s.gsub(/(\d)(?=(\d\d\d)+(?!\d))/, "\\1,"); end

  def self.hash_keys_to_symbols(object)
    case object
    when Hash
      object.inject({}) {|h, (k, v)| h[k.to_sym] = hash_keys_to_symbols(v); h}
    when Array
      object.map {|x| hash_keys_to_symbols(x)}
    else
      object
    end
  end

  def self.artists_from_cache(file)
    hash_keys_to_symbols(JSON.parse(open(file).read)).each do |key, value|
      ARTISTS[key] = value
    end
  end

  def self.artists_to_cache(file)
    cache = ARTISTS.inject({}) do |hash, (key, value)|
      hash[key] = value unless value[:countries].empty?
      hash
    end

    file = open(file, 'w')
    file.puts(cache.to_json)
    file.close
  end

  def self.tag_to_countries(tag)
    COUNTRIES.select do |country|
      (country[:name].downcase == tag.downcase ||
       country[:adjectives].map {|a| a.downcase}.include?(tag.downcase))
    end
  end

  def self.artist_countries(artist, api_response=nil)
    if ARTISTS[artist] && ARTISTS[artist][:countries]
      return ARTISTS[artist][:countries]
    end

    countries = []
    api_response ||= LastFM::Artist.get_top_tags(:artist => artist)

    [(api_response['toptags']['tag'] || [])].flatten.each do |t|
      if t['count'].to_i > 0
        countries << tag_to_countries(t['name'])
      end
    end

    countries = countries.uniq.flatten.sort_by {|c| c[:name]}

    ARTISTS[artist] ||= {}
    ARTISTS[artist][:countries] = countries
  end

  def self.user_artists(username, period='7day', limit=10_000, api_response=nil)
    if USERS[username] && USERS[username][period]
      return USERS[username][period]
    end

    user_artists = []
    api_response ||= LastFM::User.get_top_artists(:user => username,
                                                  :period => period,
                                                  :limit => limit)

    (api_response['topartists']['artist'] || []).each do |a|
      artist = a['name']
      image = a['image'].select {|x| x['size'] == 'medium'}[0]['#text']

      ARTISTS[artist] ||= {}
      ARTISTS[artist][:url] = a['url']
      ARTISTS[artist][:image] = image
      ARTISTS[artist][:countries] ||= artist_countries(artist)

      user_artists << {
        :name => a['name'],
        :playcount => a['playcount'].to_i,
      }.merge(ARTISTS[artist])
    end

    USERS[username] ||= {}
    USERS[username][period] = user_artists.sort_by {|a| a[:playcount]}.reverse
  end

  def self.group_by_country(artists)
    groups = {}

    artists.each do |artist|
      artist[:countries].each do |country|
        groups[country[:iso_3]] ||= []
        groups[country[:iso_3]] << artist
      end
    end

    groups.sort.map do |group|
      country = COUNTRIES.select {|c| c[:iso_3] === group.first}.first

      {
        :artists => group.last,
        :playcount => group.last.map {|a| a[:playcount]}.inject(0, :+),
      }.merge(country)
    end.sort_by {|c| c[:playcount]}.reverse
  end

  def self.flag_list(username, period, cache_directory, cache_for)
    cache_directory = File.join('public', cache_directory, username)

    FileUtils.mkdir_p(cache_directory)

    filename = File.join(cache_directory, "#{period}.png")

    if File.exist?(filename)
      if File.mtime(filename) > (Time.now - cache_for)
        return filename
      else
        File.delete(filename)
      end
    end

    generate_flag_list(group_by_country(user_artists(username, period)),
                       filename)
  end

  def self.generate_flag_list(groups, output)
    height = 30
    rows = groups[0..29].length / 2

    styles = {
      :font_size => 16, :font_family => 'DejaVu Sans Condensed',
      :font_style => 'normal', :font_weight => 'bold',
    }

    Magick::RVG.new(300, (height + 2) * rows) do |canvas|
      canvas.background_fill = '#eee'

      canvas.g do |body|
        groups[0..29].each_with_index do |group, i|
          x = (i % 2) * 150
          y = (i / 2) * (height + 2)

          if (i % 2 == 0) and ((i / 2) % 2 == 0)
            body.rect(300, height + 1, 0, y).styles(:fill => '#ddd')
          end

          flag = "./lib/music_mapper/flag/#{group[:iso_2].downcase}.svg"

          body.image(Magick::Image.read(flag).first, 40, height, x + 62, y + 1)

          body.text(x + 104, y + 22, group[:iso_3]).
            styles(styles)

          body.text(x + 60, y + 22, thousands(group[:playcount])).
            styles(styles.merge(:text_anchor => 'end'))
        end
      end
    end.draw.write(output)

    output
  end
end
