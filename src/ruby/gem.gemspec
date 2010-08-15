# Waxeye Parser Generator
# www.waxeye.org
# Copyright (C) 2008 Orlando D. A. R. Hill
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
# of the Software, and to permit persons to whom the Software is furnished to do
# so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

require 'rubygems'
require 'rake'

spec = Gem::Specification.new do |s|
  s.author = 'Orlando Hill'
  s.email = 'orlandodarhill Gmail.com'
  s.name = 'waxeye'
  s.version = '0.8.0'
  s.homepage = 'http://waxeye.org'
  s.summary = 'Ruby runtime for the Waxeye Parser Generator'
  s.rubyforge_project = 'waxeye'
  s.files = FileList['README', 'LICENSE', 'lib/waxeye.rb'].to_a
  s.test_files = []
end
