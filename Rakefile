Dir['*.rake'].each {|t| load(t)}

desc 'Run all specs in spec/'
task :spec do
  require 'bacon'

  Bacon.extend Bacon::TestUnitOutput
  Bacon.summary_on_exit

  Dir['spec/**/*.rb'].each {|f| require "./#{f}"}
end

desc 'Update any stale images'
task :update_images do
  require 'bundler/setup'
  require 'setup'

  Dir['public/image/**/*.png'].each do |file|
    username, period = file.split(/[\.\/]/)[2..3]

    MusicMapper.flag_list(username,
                          period,
                          SETTINGS['image']['cache_directory'],
                          SETTINGS['image']['cache_for'])
  end
end

desc 'Remove clipPaths'
task :remove_clippaths do
  Dir['./lib/music_mapper/flag/*.svg'].each do |file|
    new = open(file).read.
      gsub(/<\/?clipPath.*?\/?>/m, '').
      gsub(/\n\s*clip-path=".*?"\s*\n/, "\n")

    open(file, 'w').puts(new)
  end
end
