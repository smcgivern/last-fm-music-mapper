require 'json'
require 'sinatra'
require 'sinatra/reloader'
require 'helpers'
require 'lib/last_fm'
require 'schema'

SETTINGS = JSON.parse(open('settings.json').read)

SETTINGS['last_fm'].each do |key, value|
  LastFM::Config.send("#{key}=", value)
end

get '/' do
  if request.params['username']
    return redirect("/:#{request.params['username']}/")
  end

  haml :index
end

get '/::username/?' do
  username = params['username']
  user_artists = LastFM::User.get_top_artists(:user => username,
                                              :limit => 10_000)

  user = load_user_artists(username, user_artists)
end
