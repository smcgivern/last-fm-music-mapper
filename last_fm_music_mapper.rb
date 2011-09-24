require 'sinatra'
require 'sinatra/reloader'
require 'helpers'

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
