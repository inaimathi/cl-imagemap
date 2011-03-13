def get_relevant_tags(file_name)
  def tag(tag_name)
    /<#{tag_name} [^\/]*?\/>/
  end
  File.open(file_name).reduce{ |rest, line| rest += line }.scan(/#{tag("rect")}|#{tag("polygon")}/).each{|elem| elem.gsub!(/[\r\n\t\\]/, "")}
end

def list_add(l1, l2)
  (0..l1.length-1).to_a.map do |i|
    l1[i] + l2[i]
  end
end

def polygon_to_points(svg_tag, transform = [0, 0])
  points = svg_tag.scan(/points=".*?"/)[0].scan(/[\d\.,]+/).map do |pair_string| 
    (x, y) = pair_string.split(",")
    list_add([x.to_f.round, y.to_f.round], transform)
  end
  if points[0] != points[-1]
    points = points + points[0,1]
  end
end

def rect_to_points(svg_tag, transform = [0, 0])
  props = Hash[svg_tag.scan(/(x|y|width|height)="(\d+\.?\d*)"/).map{|(k, v)| [k, v.to_f.round]}]
  [list_add([props["x"], props["y"]], transform), 
   list_add([props["x"]+props["width"], props["y"]+props["height"]], transform)]
end

def tag_to_fill (svg_tag)
  svg_tag.scan(/fill="#(.*?)"/)[0][0]
end

def tag_to_area(svg_tag, transform = [0, 0])
  def html(type, css_class, coords)
    "<area shape=\"#{type}\" class=\"#{css_class}\" coords=\"#{coords.join(",")}\" href=\"#\" />"
  end
  css_class = "color-#{tag_to_fill(svg_tag)}"
  if svg_tag =~ /<polygon/
    html("poly", css_class, polygon_to_points(svg_tag, transform))
  elsif svg_tag =~ /<rect/
    html("rect", css_class, rect_to_points(svg_tag, transform))
#  elsif test =~ /<circle/
  end
end

def file_to_imagemap(file_name, map_name, transform = [0, 0])
  "<map name=\"#{map_name}\">
\t#{get_relevant_tags(file_name).map{|tag| tag_to_area(tag, transform)}.join("\n\t")}
</map>"
end
