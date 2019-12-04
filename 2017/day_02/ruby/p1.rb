#!/usr/bin/env ruby
# frozen_string_literal: true

script_dir = __dir__
filename = format('%<script_dir>s/../input.txt', script_dir: script_dir)
input = File.readlines(filename)

checksum = 0

input.each do |line|
  max = -1
  min = -1

  line.split("\t").each do |val|
    val_int = val.to_i
    max = max == -1 || max < val_int ? val_int : max
    min = min == -1 || min > val_int ? val_int : min
  end

  checksum += max - min
end

puts format('The checksum is %<checksum>d', checksum: checksum)
