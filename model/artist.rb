require 'digest'

class Artist
  include DataMapper::Resource

  has n, :tags
  has n, :user_artists
  has n, :countries, :through => Resource

  property :music_brainz_id, String, :length => 36, :key => true
  property :name, String
  property :url, String
  property :image, String
  property :updated_at, Time

  def self.make_mbid(artist)
    return artist['mbid'] if artist['mbid'].length > 0

    "md5:#{Digest::MD5.hexdigest(artist['name'])}"
  end
end
