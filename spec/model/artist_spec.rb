require './spec/setup'
require './model/schema'

describe 'Artist::make_mbid' do
  it 'should use the MusicBrainz ID if it exists' do
    Artist.make_mbid({'mbid' => 'wpujc-vaas', 'name' => 'Chaminda'}).
      should.equal 'wpujc-vaas'
  end

  it 'should generate an MD5 hash from the name otherwise' do
    Artist.make_mbid({'mbid' => '', 'name' => ''}).
      should.equal 'md5:d41d8cd98f00b204e9800998ecf8427e'
  end

  it 'should generate a value no more than 36 characters long' do
    Artist.make_mbid({'mbid' => '7e35659f-f3bb-4c28-bcf5-47bd5cfa2bca'}).length.
      should.equal 36

    Artist.make_mbid({'mbid' => '', 'name' => ''}).length.
      should.equal 36
  end
end
