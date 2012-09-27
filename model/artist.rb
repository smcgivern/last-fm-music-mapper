require 'digest'

class Artist
  include DataMapper::Resource

  has n, :tags, :through => Resource
  has n, :user_artists
  has n, :countries, :through => Resource

  property :music_brainz_id, String, :length => 36, :key => true
  property :name, String
  property :url, String
  property :image, String
  property :updated_at, Time

  def load_tags(api_response)
    if ((tags.length > 0 and updated_at >= (Time.now - 7 * 24 * 60 * 60)) or
        (api_response['toptags']['@attr']['artist'] != name))
      return self
    end

    api_response['toptags']['tag'].each do |t|
      tags << Tag.first_or_create(:tag => t['name'])
    end

    tags.save!

    self
  end

  def self.make_mbid(artist)
    return artist['mbid'] if artist['mbid'].length > 0

    "md5:#{Digest::MD5.hexdigest(artist['name'])}"
  end
end
