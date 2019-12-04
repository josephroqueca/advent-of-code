#!/usr/bin/env ruby
# frozen_string_literal: true

script_dir = __dir__
filename = format('%<script_dir>s/../input.txt', script_dir: script_dir)
input = File.readlines(filename)
input = input[0]

sum = 0
input.split('').each_with_index do |c, i|
  sum += c.to_i if c == input[(i + input.length / 2) % input.length]
end

puts format('The captcha is %<sum>d', sum: sum)
