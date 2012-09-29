require 'json'
require './lib/music_mapper'

unless defined? SETTINGS
  SETTINGS = JSON.parse(open('settings.json').read)
end

if SETTINGS['last_fm']
  SETTINGS['last_fm'].each {|k, v| LastFM::Config.send("#{k}=", v)}
end
