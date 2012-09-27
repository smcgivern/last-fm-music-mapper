require 'bacon'
require 'bundler/setup'

SETTINGS = {'database' => {
    'url' => 'sqlite::memory:',
    'log_file' => "tmp/spec-datamapper.log",
    'log_level' => 'debug',
  }}

require './setup'
