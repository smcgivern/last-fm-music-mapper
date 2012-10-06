require 'kramdown'
require 'haml'
require 'sass'
require 'sinatra'
require 'sinatra/reloader'

require './helpers'

get '/' do
  if request.params['username']
    return redirect("/:#{request.params['username']}/")
  end

  @page_title = 'Last.fm music mapper'

  haml :index
end

get '/ext/style.css' do
  scss :style
end

get '/::username/:period/?' do
  @username = params['username'].match(/[a-z0-9_\-]+/i)[0]

  @period = (MusicMapper::PERIODS.select do |period|
    period[:identifier] == params['period']
  end + MusicMapper::PERIODS).first

  @user_artists = MusicMapper.user_artists(@username, @period[:identifier])

  return haml(:no_results) if @user_artists.empty?

  image_path = MusicMapper.flag_list(@username,
                                     @period[:identifier],
                                     SETTINGS['image']['cache_directory'],
                                     SETTINGS['image']['cache_for'])

  @page_title = "Last.fm music map for #{@username}"
  @scripts = ['/ext/masonry.min.js', '/ext/music-mapper.js']
  @image = r(image_path.sub(/\Apublic/, ''))
  @self = r("/:#{@username}/#{@period[:identifier]}/")

  haml :user
end
