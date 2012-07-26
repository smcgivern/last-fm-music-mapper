require 'rack'
require 'last_fm_music_mapper'
require 'ruby-prof'

use Rack::RubyProf, :path => 'tmp/profile/'

run Sinatra::Application
