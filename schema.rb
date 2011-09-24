require 'dm-core'
require 'dm-migrations'

DataMapper::Logger.new($stdout, :debug)
DataMapper.setup(:default,
                 :adapter  => 'sqlite3',
                 :database => 'last_fm_music_mapper.sqlite')

class User
  include DataMapper::Resource

  has n, :user_artists

  property :username, String, :key => true
  property :updated_at, DateTime
end

class Artist
  include DataMapper::Resource

  has n, :artist_images
  has n, :tags
  has n, :user_artists
  has n, :countries, :through => Resource

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

class Tag
  include DataMapper::Resource

  has n, :artists, :through => Resource
  has n, :tag_countries

  property :tag, String, :key => true
end

class Country
  include DataMapper::Resource

  has n, :tag_countries

  property :iso_code, String, :key => true
  property :name, String
  property :updated_at, DateTime
end

class UserArtist
  include DataMapper::Resource

  belongs_to :user, :key => true
  belongs_to :artist, :key => true

  property :period, String, :key => true
  property :play_count, Numeric
end

class TagCountry
  include DataMapper::Resource

  belongs_to :tag, :key => true
  belongs_to :country, :key => true
end

DataMapper.finalize
DataMapper.auto_migrate!
