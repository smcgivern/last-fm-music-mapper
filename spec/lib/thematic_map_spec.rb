require 'spec/setup'
require 'lib/thematic_map'

describe 'ThematicMap.new' do
end

describe 'ThematicMap#map_file' do
  it 'should return the map file given in @map, if a string' do
    ThematicMap.new {|m| m.map = '~/europe_map.xml'}.map_file.
      should.equal '~/europe_map.xml'
  end

  it 'should pick a SVG file from lib/thematic_map/maps/, if a symbol' do
    ThematicMap.new {|m| m.map = :asia_map}.map_file.
      should.equal './lib/thematic_map/maps/asia_map.svg'
  end
end

describe 'ThematicMap#map_xml' do
  it 'should be a Nokogiri XML document of #map_xml' do
    ThematicMap.new {|m| m.map = 'spec/fixture/thematic_map.xml'}.map_xml.
      should.be.a lambda {|x| Nokogiri::XML::Document === x}
  end
end
