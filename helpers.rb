set :haml, {:format => :html5}
set :views, "#{File.dirname(__FILE__)}/view"
LastFM::Config.api_key = SETTINGS['last_fm']['api_key']

def load_user_artists(user_artists, period='overall')
  user = User.first_or_create(:username =>
                              user_artists['topartists']['@attr']['user'])

  user_artists['topartists']['artist'].each do |a|
    artist = Artist.first_or_create(:music_brainz_id => a['mbid'])

    # May as well keep these current every time.
    artist.update(:name => a['name'], :url => a['url'])

    a['image'].each do |i|
      ArtistImage.first_or_create(:artist => artist, :url => i['#text'],
                                  :size => i['size'])
    end

    UserArtist.first_or_create(:user => user, :artist => artist,
                               :period => period, :play_count => a['playcount'])
  end

  user.reload
end

helpers do
  include Rack::Utils
  alias_method :h, :escape_html
end
