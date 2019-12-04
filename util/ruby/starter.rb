#!/usr/bin/env ruby
# frozen_string_literal: true

# rubocop:disable Lint/UselessAssignment

script_dir = __dir__
filename = format('%<script_dir>s/../input.txt', script_dir: script_dir)
input = File.readlines(filename)

# rubocop:enable Lint/UselessAssignment
