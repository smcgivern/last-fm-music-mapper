require './lib/last_fm'
require './lib/music_mapper/countries'

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

  def self.hash_keys_to_symbols(object)
    case object
    when Hash
      object.inject({}) {|h, (k, v)| h[k.to_sym] = hash_keys_to_symbols(v); h}
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

    open(file, 'w').puts(cache.to_json)
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

    (api_response['toptags']['tag'] || []).each do |t|
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
    USERS[username][period] = user_artists
  end
end
