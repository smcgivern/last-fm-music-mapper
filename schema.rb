require 'dm-core'
require 'dm-migrations'
require 'dm-timestamps'

DataMapper::Property::String.length(255)
DataMapper::Logger.new($stdout, :debug)
DataMapper.setup(:default, 'sqlite::memory:')

require './model/artist'
require './model/country'
require './model/period'
require './model/tag'
require './model/user'
require './model/associations'

DataMapper.finalize
DataMapper.auto_migrate!

require './model/seed_data'
