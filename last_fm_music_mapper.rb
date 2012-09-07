require 'haml'
require 'sinatra'
require 'sinatra/reloader'

require './helpers'

get '/' do
  if request.params['username']
    return redirect("/:#{request.params['username']}/")
  end

  haml :index
end

get '/::username/?' do
  @user = User.first_or_create(:username => params['username'])

  user_artists = LastFM::User.get_top_artists(:user => @user.username,
                                              :period => '7day',
                                              :limit => 10_000)

  @user.load_artists(user_artists)

  haml :user
end
