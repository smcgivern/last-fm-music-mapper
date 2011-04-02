require 'dm-core'
require 'dm-migrations'

DataMapper::Logger.new($stdout, :debug)
DataMapper.setup(:default, 'sqlite::memory:')

class User
  include DataMapper::Resource

  property :username, String, :key => true
  property :updated_at, DateTime
end

class Artist
  include DataMapper::Resource

  has n, :countries

  property :music_brainz_id, String, :key => true
  property :name, String
  property :url, String
  property :updated_at, DateTime
end

class ArtistImage
  include DataMapper::Resource

  belongs_to :artist

  property :url, String, :key => true
  property :size, String
end

class UserArtist
  include DataMapper::Resource

  belongs_to :user, :key => true
  belongs_to :artist, :key => true

  property :period, String, :key => true
  property :play_count, Numeric
end

class Country
  include DataMapper::Resource

  property :iso_code, String, :key => true
  property :updated_at, DateTime
end

DataMapper.finalize
DataMapper.auto_migrate!
