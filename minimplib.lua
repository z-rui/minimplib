-- This is file `minimplib.lua',
-- manually stripped from `luamplib.lua'.
--
-- See source file of luamplib for licencing and contact information.

luamplib          = luamplib or { }

local luamplib    = luamplib
local tex         = tex

local format, abs = string.format, math.abs

local err  = function(s) return tex.error(s) end
local warn = function(s) return tex.error(s) end
local info = function(s) return texio.write_nl("log", s) end

local tableconcat   = table.concat

local mplib = require ('mplib')
local kpse  = require ('kpse')
local lfs   = require ('lfs')

local ioopen        = io.open

local file = file or { }
local replacesuffix = file.replacesuffix or function(filename, suffix)
  return filename:gsub("%.[%a%d]+$", "") .. "." .. suffix
end

local is_writable = file.is_writable or function(name)
  if lfs.isdir(name) then
    name = name .. "/_luam_plib_temp_file_"
    local fh = ioopen(name,"w")
    if fh then
      fh:close(); os.remove(name)
      return true
    end
  end
end
local mk_full_path = lfs.mkdirs or function(path)
  local full = ""
  for sub in path:gmatch("(/*[^\\/]+)") do
    full = full .. sub
    lfs.mkdir(full)
  end
end

local outputdir
if lfs.touch then
  local texmfvar = kpse.expand_var('$TEXMFVAR')
  if texmfvar and texmfvar ~= "" and texmfvar ~= '$TEXMFVAR' then
    for dir in texmfvar:gmatch(os.type == "windows" and "[^;]+" or "[^:]+") do
      if not lfs.isdir(dir) then
        mk_full_path(dir)
      end
      if is_writable(dir) then
        local cached = dir .. "/luamplib_cache"
        lfs.mkdir(cached)
        outputdir = cached
        break
      end
    end
  end
end
if not outputdir then
  outputdir = "."
  for _,v in ipairs(arg) do
    local t = v:match("%-output%-directory=(.+)")
    if t then
      outputdir = t
      break
    end
  end
end

local begname = "%f[A-Z_a-z]"
local endname = "%f[^A-Z_a-z]"

local btex_etex        = begname.."btex"..endname.."%s*(.-)%s*"..begname.."etex"..endname
local verbatimtex_etex = begname.."verbatimtex"..endname.."%s*(.-)%s*"..begname.."etex"..endname

local TeXsnippets = {}

local function preprocess(data)
  return data:gsub(btex_etex, function(s)
    local id = #TeXsnippets + 1
    TeXsnippets[id] = s
    return ([[MPlibTeX_(%d,)]]):format(id)
  end):gsub(verbatimtex_etex, "")
end

-- These has no btex ... etex so no need to substitute
local whitelist = {
  ["boxes.mp"] = true,
  ["graph.mp"] = true,
  ["marith.mp"] = true,
  ["mfplain.mp"] = true,
  ["mpost.mp"] = true,
  ["plain.mp"] = true,
  ["rboxes.mp"] = true,
  ["sarith.mp"] = true,
  ["string.mp"] = true,
  ["TEX.mp"] = true,
}
-- "format.mp" (and hence "graph.mp", etc) is pretty broken,
-- due to the way luamplib hacks with the labels is quite unexpected.
local blacklist = {
  ["format.mp"] = function(data) return data..[[
vardef Fdec_o_(expr x) = TEX("$"&decimal x&"$") enddef;
vardef Fsci_o_(expr x, e) =
  TEX("$"& if x=-1: "-"& elseif x<>1: decimal x &"{\times}"& fi
      "10^{"&decimal e&"}$")
enddef;
]] end
}

local cache_files = {}
local function replaceinputmpfile (name,file)
  local cached = cache_files[file]
  if cached then return cached end
  local cachedir = luamplib.cachedir or outputdir
  local newfile = name:gsub("%W","_")
  newfile = cachedir .."/luamplib_input_"..newfile

  local fh = ioopen(file,"r")
  if not fh then return file end

  data = fh:read("*all"); fh:close()
  data = (blacklist[name] or preprocess)(data)

  fh = ioopen(newfile,"w")
  if not fh then return file end
  fh:write(data); fh:close()
  cache_files[file] = newfile
  return newfile
end

local randomseed = nil

local mpkpse = kpse.new("luatex", "mpost")

local special_ftype = {
  pfb = "type1 fonts",
  enc = "enc files",
}

local function finder(name, mode, ftype)
  if mode == "w" then
    return name
  else
    ftype = special_ftype[ftype] or ftype
    local file = mpkpse:find_file(name,ftype)
    if file then
      if ftype ~= "mp" or whitelist[name] then return file end
      return replaceinputmpfile(name,file)
    end
    return mpkpse:find_file(name,name:match("[a-zA-Z]+$"))
  end
end
luamplib.finder = finder

local preamble = [[
boolean mplib; mplib := true;
input %s;

vardef MPlibTeX_(expr id)(text t) =
  image ( addto currentpicture also ("#!" & decimal id) infont defaultfont
    withprescript ("MPlibTeX=" & decimal id) t;
    if known TEXBOX_[id]:
      save dimen; color dimen;
      dimen := TEXBOX_[id];
      setbounds currentpicture to unitsquare
        xscaled redpart dimen
        yscaled (greenpart dimen + bluepart dimen)
        shifted (0, -bluepart dimen)
    fi)
enddef;

TEXBOX_id := 0;
vardef TEX(expr t) =
  TEXBOX_id := TEXBOX_id - 1;
  MPlibTeX_(TEXBOX_id, withprescript "MPlibmkTEXbox="&t)
enddef;
]]

function luamplib.load(name)
  local mpx = mplib.new {
    ini_version = true,
    find_file = luamplib.finder,
    math_mode = luamplib.numbersystem,
    random_seed = randomseed,
  }
  local result
  if not mpx then
    result = { status = 99, error = "out of memory"}
  else
    result = mpx:execute(format(preamble, replacesuffix(name,"mp")))
  end
  luamplib.reporterror(result)
  return mpx, result
end

luamplib.reporterror = function (result)
  if not result then
    err("no result object returned")
  else
    local t, e, l = result.term, result.error, result.log
    local log = (t or l or "no-term"):gsub("^%s+", "\n")
    if result.status > 0 then
      warn(log)
      if result.status > 1 then
        err(e or "see above messages")
      end
    end
    return log
  end
end

local function process_indeed (mpx, data, indeed)
  local converted, result = false, {}
  if mpx and data then
    result = mpx:execute(data)
    local log = luamplib.reporterror(result)
    if indeed and log then
      if result.fig then
        if log:find("\n>>") then info(log) end
        converted = luamplib.convert(result)
      else
        info(log)
        warn("No figure output. Maybe no beginfig/endfig")
      end
    end
  else
    err("Mem file unloadable. Maybe generated with a different version of mplib?")
  end
  return converted, result
end

local currentformat = "plain"

local process = function (data,indeed)
  local firstpass = not indeed
  if firstpass then
    cache_files = {} -- XXX: need to clear cache when a new file is processed
  end
  randomseed = firstpass and math.random(65535) or randomseed
  mpx = luamplib.load(currentformat)
  return process_indeed(mpx, data, indeed)
end
luamplib.process = process

local function convert(result, flusher)
  luamplib.flush(result, flusher)
  return true -- done
end
luamplib.convert = convert

local catcode = tex.get("catcodetable")

local function pdf_startfigure(...)
  return tex.sprint(catcode, format("\\mp@start{%d}{%f}{%f}{%f}{%f}",...))
end

local function pdf_stopfigure()
  return tex.sprint(catcode, "\\mp@stop")
end

local function pdf_save()
  return tex.sprint(catcode, "\\mp@pdfsave")
end

local function pdf_restore()
  return tex.sprint(catcode, "\\mp@pdfrestore")
end

local function pdf_literalcode(...)
  return tex.sprint(catcode, "\\mp@pdf{", format(...), "}")
end
luamplib.pdf_literalcode = pdf_literalcode

local function pdf_textfigure(font,size,text,width,height,depth)
  -- BUG: char(0) is always gone
  tex.sprint(catcode, "\\mp@text{")
  tex.sprint(font, "}{", size, "bp}{")
  for c in text:gmatch(".") do
    tex.sprint("\\hbox{\\char", c:byte(), "}") -- kerning happens in metapost
  end
  tex.sprint("}")
end
luamplib.pdf_textfigure = pdf_textfigure

local bend_tolerance = 131/65536

local rx, sx, sy, ry, tx, ty, divider = 1, 0, 0, 1, 0, 0, 1

local function pen_characteristics(object)
  local t = mplib.pen_info(object)
  rx, ry, sx, sy, tx, ty = t.rx, t.ry, t.sx, t.sy, t.tx, t.ty
  divider = sx*sy - rx*ry
  return not (sx==1 and rx==0 and ry==0 and sy==1 and tx==0 and ty==0), t.width
end

local function concat(px, py) -- no tx, ty here
  return (sy*px-ry*py)/divider,(sx*py-rx*px)/divider
end

local function curved(ith,pth)
  local d = pth.left_x - ith.right_x
  if abs(ith.right_x - ith.x_coord - d) <= bend_tolerance and abs(pth.x_coord - pth.left_x - d) <= bend_tolerance then
    d = pth.left_y - ith.right_y
    if abs(ith.right_y - ith.y_coord - d) <= bend_tolerance and abs(pth.y_coord - pth.left_y - d) <= bend_tolerance then
      return false
    end
  end
  return true
end

local function flushnormalpath(path,open)
  local pth, ith
  for i=1,#path do
    pth = path[i]
    if not ith then
      pdf_literalcode("%f %f m",pth.x_coord,pth.y_coord)
    elseif curved(ith,pth) then
      pdf_literalcode("%f %f %f %f %f %f c",ith.right_x,ith.right_y,pth.left_x,pth.left_y,pth.x_coord,pth.y_coord)
    else
      pdf_literalcode("%f %f l",pth.x_coord,pth.y_coord)
    end
    ith = pth
  end
  if not open then
    local one = path[1]
    if curved(pth,one) then
      pdf_literalcode("%f %f %f %f %f %f c",pth.right_x,pth.right_y,one.left_x,one.left_y,one.x_coord,one.y_coord )
    else
      pdf_literalcode("%f %f l",one.x_coord,one.y_coord)
    end
  elseif #path == 1 then
    -- special case .. draw point
    local one = path[1]
    pdf_literalcode("%f %f l",one.x_coord,one.y_coord)
  end
  return t
end

local function flushconcatpath(path,open)
  pdf_literalcode("%f %f %f %f %f %f cm", sx, rx, ry, sy, tx ,ty)
  local pth, ith
  for i=1,#path do
    pth = path[i]
    if not ith then
      pdf_literalcode("%f %f m",concat(pth.x_coord,pth.y_coord))
    elseif curved(ith,pth) then
      local a, b = concat(ith.right_x,ith.right_y)
      local c, d = concat(pth.left_x,pth.left_y)
      pdf_literalcode("%f %f %f %f %f %f c",a,b,c,d,concat(pth.x_coord, pth.y_coord))
    else
      pdf_literalcode("%f %f l",concat(pth.x_coord, pth.y_coord))
    end
    ith = pth
  end
  if not open then
    local one = path[1]
    if curved(pth,one) then
      local a, b = concat(pth.right_x,pth.right_y)
      local c, d = concat(one.left_x,one.left_y)
      pdf_literalcode("%f %f %f %f %f %f c",a,b,c,d,concat(one.x_coord, one.y_coord))
    else
      pdf_literalcode("%f %f l",concat(one.x_coord,one.y_coord))
    end
  elseif #path == 1 then
    -- special case .. draw point
    local one = path[1]
    pdf_literalcode("%f %f l",concat(one.x_coord,one.y_coord))
  end
  return t
end

local function script2table(s)
  local t = {}
  for i in s:gmatch("[^\13]+") do
    local k,v = i:match("(.-)=(.*)") -- v may contain = or empty.
    if k and v and k ~= "" then
      t[k] = v
    end
  end
  return t
end

local _boxid = {}

local function domakeTEXboxes (data)
  local locbox = tex.count[274] -- e-TeX local box allocator
  if data and data.fig then
    local figures = data.fig
    for f=1, #figures do
      local objects = figures[f]:objects()
      if objects then
        for o=1,#objects do
          local prescript = objects[o].prescript
          prescript = prescript and script2table(prescript)
          local id = prescript and tonumber(prescript.MPlibTeX)
          if id and not _boxid[id] then
            local str = id > 0 and TeXsnippets[id] or prescript.MPlibmkTEXbox
            tex.sprint(catcode, "\\mp@locbox{", id, "}{")
            tex.sprint(str, "}")
            locbox = locbox - 1
            _boxid[id] = locbox
          end
        end
      end
    end
  end
end

local function makeTEXboxes (data)
  local _,result = process(data, false)
  domakeTEXboxes(result)
  return data
end
luamplib.makeTEXboxes = makeTEXboxes

local function processwithTEXboxes (data)
  if not data then return end
  local buf = { "color TEXBOX_[];\n" }
  for id, box in pairs(_boxid) do
    local box = tex.getbox(box)
    if box then
      local line = format(
        "TEXBOX_[%i]:=(%f,%f,%f) * pt;\n",
        id, box.width /65536, box.height/65536, box.depth /65536)
      -- no longer need these dimensions
      box.width = 0
      box.height = 0
      box.depth = 0
      buf[#buf+1] = line
    end
  end
  buf[#buf+1] = data
  process(tableconcat(buf), true)
end
luamplib.processwithTEXboxes = processwithTEXboxes

local function colorconverter(cr)
  local n = #cr
  if n == 4 then
    local c, m, y, k = cr[1], cr[2], cr[3], cr[4]
    return "%.3f %.3f %.3f %.3f k %.3f %.3f %.3f %.3f K",c,m,y,k,c,m,y,k
  elseif n == 3 then
    local r, g, b = cr[1], cr[2], cr[3]
    return "%.3f %.3f %.3f rg %.3f %.3f %.3f RG",r,g,b,r,g,b
  else
    local s = cr[1]
    return "%.3f g %.3f G",s,s
  end
end

local function do_preobj_color(object,prescript)
    local cs = object.color
    if cs and #cs > 0 then
      pdf_literalcode(colorconverter(cs))
    end
end

local function flush(result,flusher)
  if result then
    local figures = result.fig
    if figures then
      for f=1, #figures do
        local figure = figures[f]
        local objects = figure:objects()
        local fignum = figure:charcode() or tonumber(figure:filename():match("([%d]+)$")) or 0
        local miterlimit, linecap, linejoin, dashed = -1, -1, -1, false
        local bbox = figure:boundingbox()
        local llx, lly, urx, ury = bbox[1], bbox[2], bbox[3], bbox[4] -- faster than unpack
        if urx < llx then
          -- invalid
          pdf_startfigure(fignum,0,0,0,0)
          pdf_stopfigure()
        else
          pdf_startfigure(fignum,llx,lly,urx,ury)
          pdf_save()
          if objects then
            for o=1,#objects do
              local object        = objects[o]
              local objecttype    = object.type
              local prescript     = object.prescript
              prescript = prescript and script2table(prescript) -- prescript is now a table
              do_preobj_color(object,prescript)
              if objecttype == "start_bounds" or objecttype == "stop_bounds" then
                -- skip
              elseif objecttype == "start_clip" then
                pdf_save()
                flushnormalpath(object.path,t,false)
                pdf_literalcode("W n")
              elseif objecttype == "stop_clip" then
                pdf_restore()
                miterlimit, linecap, linejoin, dashed = -1, -1, -1, false
              elseif objecttype == "text" then
                local id = prescript and tonumber(prescript.MPlibTeX)
                local ot = object.transform -- 3,4,5,6,1,2
                pdf_save()
                pdf_literalcode("%f %f %f %f %f %f cm",ot[3],ot[4],ot[5],ot[6],ot[1],ot[2])
                if id then
                  tex.sprint("\\copy", _boxid[id], "\\relax")
                else
                  pdf_textfigure(object.font,object.dsize,object.text,object.width,object.height,object.depth)
                end
                pdf_restore()
              else
                local ml = object.miterlimit
                if ml and ml ~= miterlimit then
                  miterlimit = ml
                  pdf_literalcode("%f M",ml)
                end
                local lj = object.linejoin
                if lj and lj ~= linejoin then
                  linejoin = lj
                  pdf_literalcode("%i j",lj)
                end
                local lc = object.linecap
                if lc and lc ~= linecap then
                  linecap = lc
                  pdf_literalcode("%i J",lc)
                end
                local dl = object.dash
                if dl then
                  local d = format("[%s] %i d",tableconcat(dl.dashes or {}," "),dl.offset)
                  if d ~= dashed then
                    dashed = d
                    pdf_literalcode(dashed)
                  end
                elseif dashed then
                  pdf_literalcode("[] 0 d")
                  dashed = false
                end
                local path = object.path
                local transformed, penwidth = false, 1
                local open = path and path[1].left_type and path[#path].right_type
                local pen = object.pen
                if pen then
                  if pen.type == 'elliptical' then
                    transformed, penwidth = pen_characteristics(object) -- boolean, value
                    pdf_literalcode("%f w",penwidth)
                    if objecttype == 'fill' then
                      objecttype = 'both'
                    end
                  else -- calculated by mplib itself
                    objecttype = 'fill'
                  end
                end
                if transformed then
                  pdf_save()
                end
                if path then
                  if transformed then
                    flushconcatpath(path,open)
                  else
                    flushnormalpath(path,open)
                  end
                  if objecttype == "fill" then
                    pdf_literalcode("h f")
                  elseif objecttype == "outline" then
                    pdf_literalcode((open and "S") or "h S")
                  elseif objecttype == "both" then
                    pdf_literalcode("h B")
                  end
                end
                if transformed then
                  pdf_restore()
                end
                local path = object.htap
                if path then
                  if transformed then
                    pdf_save()
                  end
                  if transformed then
                    flushconcatpath(path,open)
                  else
                    flushnormalpath(path,open)
                  end
                  if objecttype == "fill" then
                    pdf_literalcode("h f")
                  elseif objecttype == "outline" then
                    pdf_literalcode((open and "S") or "h S")
                  elseif objecttype == "both" then
                    pdf_literalcode("h B")
                  end
                  if transformed then
                    pdf_restore()
                  end
                end
              end
            end
          end
          pdf_restore()
          pdf_stopfigure()
        end
      end
    end
  end
end
luamplib.flush = flush
-- vim: ts=2:sw=2:et
