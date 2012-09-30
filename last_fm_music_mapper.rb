require 'haml'
require 'sass'
require 'sinatra'
require 'sinatra/reloader'

require './helpers'

get '/' do
  if request.params['username']
    return redirect("/:#{request.params['username']}/")
  end

  haml :index
end

get '/ext/style.css' do
  scss :style
end

get '/::username/?' do
  @user_artists = MusicMapper.user_artists(params['username'],
                                           '7day')

  haml :user
end
