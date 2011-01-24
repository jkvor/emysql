# sed command file to convert markdown format to edoc format
# to make markdown readme files part of an edoc overview.
# Sample: sed -E -f <this file> README.markdown > overview.edoc
# Markdown: http://daringfireball.net/projects/markdown/
# Edoc: http://www.erlang.org/doc/apps/edoc/ 
# Eonblast: http://www.eonblast.com
# Public domain, experimental, no warranties

# external links
s/\[([^]]+)\]\(([^)]+)\)/<a href=\"\2\">\1<\/a>/

# headlines
s/^####(.+)$/====\1 ====/
s/^###(.+)$/===\1 ===/
s/^##(.+)$/==\1 ==/
s/^#(.+)$/=\1 =/

# bullet points (edoc must see </li>)
s/^\*(.+)$/<li>\1<\/li>/

# source: each tabbed line
s/^	(.+)$/```	\1'''/

# emails
s/<([^@>]+@[^.>]+.[^>]+)>/<a href=\"mailto:\1\">\1<\/a>/

# line breaks
s/  $/<br \/>/