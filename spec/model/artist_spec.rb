require 'spec/setup'
require 'schema'

describe 'Artist::make_mbid' do
  it 'should use the MusicBrainz ID if it exists' do
    Artist.make_mbid({'mbid' => 'wpujc-vaas', 'name' => 'Chaminda'}).
      should.equal 'wpujc-vaas'
  end

  it 'should generate a SHA-1 hash from the name otherwise' do
    Artist.make_mbid({'mbid' => '', 'name' => ''}).
      should.equal 'sha:da39a3ee5e6b4b0d3255bfef95601890afd80709'
  end
end
