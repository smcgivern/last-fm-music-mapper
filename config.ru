require 'ruby-prof'

require 'bundler/setup'
require 'rack'

require './setup'
require './last_fm_music_mapper'

use Rack::RubyProf, :path => 'tmp/profile/'

run Sinatra::Application
