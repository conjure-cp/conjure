#!/usr/bin/env ruby
class String
  def colourise(color_code)
    "\e[#{color_code}m#{self}\e[0m"
  end

  def red
    colourise(31)
  end

  def green
    colourise(32)
  end

  def yellow
    colourise(33)
  end

  def pink
    colourise(35)
  end
end


ARGF.each_line do |line|
	line=line.gsub "\\n", "\n"
	line.gsub! "FAILED", "FAILED".red
	puts line
end