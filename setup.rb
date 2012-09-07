require 'data_mapper'
require 'json'

require './lib/last_fm'

SETTINGS ||= JSON.parse(open('settings.json').read)

if SETTINGS['last_fm']
  SETTINGS['last_fm'].each {|k, v| LastFM::Config.send("#{k}=", v)}
end

if SETTINGS['database']['log_level']
  SETTINGS['database']['log_file'] ||= $stdout

  DataMapper::Logger.new(SETTINGS['database']['log_file'],
                         SETTINGS['database']['log_level'])
end

DataMapper.setup(:default, SETTINGS['database']['url'])
DataMapper::Property::String.length(255)

require './model/schema'
