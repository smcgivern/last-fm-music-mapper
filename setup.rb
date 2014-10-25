require 'json'
require './lib/music_mapper'

Encoding.default_external = Encoding::UTF_8

unless defined? SETTINGS
  SETTINGS = JSON.parse(open('settings.json').read)
end

if SETTINGS['last_fm']
  SETTINGS['last_fm'].each {|k, v| LastFM::Config.send("#{k}=", v)}
end

if SETTINGS['artist'] && SETTINGS['artist']['cache_file']
  if File.exist?(SETTINGS['artist']['cache_file'])
    MusicMapper.artists_from_cache(SETTINGS['artist']['cache_file'])
  end

  at_exit do
    MusicMapper.artists_to_cache(SETTINGS['artist']['cache_file'])
  end
end
