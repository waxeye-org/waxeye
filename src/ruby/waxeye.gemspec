# Waxeye Parser Generator
# www.waxeye.org
# Copyright (C) 2008-2010 Orlando Hill
# Licensed under the MIT license. See 'LICENSE' for details.

require 'rubygems'
require 'rake'

spec = Gem::Specification.new do |s|
  s.author = 'Orlando Hill'
  s.email = 'orlandodarhill Gmail.com'
  s.name = 'waxeye'
  s.version = '0.8.1'
  s.homepage = 'http://waxeye.org'
  s.summary = 'Ruby runtime for the Waxeye Parser Generator'
  s.description = 'Waxeye is a parser generator based on parsing expression grammars (PEGs).'
  s.rubyforge_project = 'waxeye'
  s.files = FileList['README.md', 'LICENSE', 'lib/waxeye.rb'].to_a
  s.test_files = []
end
