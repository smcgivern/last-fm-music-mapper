require 'bundler/setup'
require 'rack'

require './setup'
require './last_fm_music_mapper'

run Sinatra::Application
