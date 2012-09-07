require './model/artist'
require './model/country'
require './model/period'
require './model/tag'
require './model/user'
require './model/associations'

DataMapper.finalize
DataMapper.auto_migrate!

require './model/seed_data'
