#? strongSpaces
# Copyright (C) 2015, René du R. Sacramento. All rights reserved.
# Licensed under MIT License. Look at license.txt for more info.


## The ``memviews`` module implements a very thin wrapper around array like
## types in Nim or any contiguous memory region you have a pointer to.
## It can be used to make shallow slices of your data, have a uniform interface
## to both seq and string (among other types) and acess C arrays more naturally
## and with optional bounds checking.
##
## Be carefull with the lifetime of the allocations you are viewing into, as
## the view possess only an unmanaged pointer to the data. If the GC collects
## your data, or you explicily deallocate it, this pointer may become invalid
## and undefined behaviour ensues. You may have to use GC_ref() and GC_unref()
## to manually stick the memory around.
##
## The same may happen if the data is reallocated to another point in the
## memory. For example, when a sequence has to grow as a result of ".add()".
## One should abstain to change the original seq in those ways.
##
## MemViews don't suport automatic growth as it don't owns the data it is
## pointing to. However, you can mutate any point of this memory.
##
## It is similar to Adrian Veith ``seqslices``, but much simpler, less safe and
## more general.
##
## :Author: René du R. Sacramento
## :Copyright: 2015-2018

import c_alikes, strutils

type
  MemView* {.final, pure, shallow.} [T] = object
    data: ptr T
    len*: int  ## You can access directly to retrive or change the lenght
               ## of a MemView. Be careful as there is no bounds checking
               ## when changing the lenght.
               ## Use the slicing operation for safety.

type SomeIndex = SomeInteger | BackwardsIndex

template `^^`(s, i: untyped): untyped =
  (when i is BackwardsIndex: s.len - int(i) else: int(i))

template xBoundsCheck(s, i) =
  # Bounds check for the array like acceses.
  when compileOption("boundChecks"):  # d:release should disable this.
    if unlikely(i >= s.len or i < 0): 
      raise newException(IndexError,
                         "Out of bounds: " & $i & " > " & $(s.len-1))
  discard

#proc `[]`*[T](s: MemView[T], i: int) : T {.inline.} =
#  ## Array like access of element ``i``.
#  xBoundsCheck(s, i)
#  return s.data[i]

proc `[]`*[T](s: MemView[T], i: SomeIndex): var T {.inline.} =
  ## Mutable array like access of element ``i``.
  xBoundsCheck(s, s^^i)
  return s.data[s^^i]

proc `[]=`* [T] (s: MemView[T], i: SomeIndex, val : T) {.inline.} =
  ## Change element ``i`` of the view.
  xBoundsCheck(s, s^^i)
  s.data[s^^i] = val

iterator items*[T](s: MemView[T]): T {.inline.} =
  ## Iterate over all items of a view.
  for i in 0 ..< s.len:
    yield s.data[i]

iterator mitems*[T](s: MemView[T]): var T {.inline.} =
  ## Iterate over all items of a view and be capable to change them in place.
  for i in 0 ..< s.len:
    yield s.data[i]

iterator pairs*[T](s: MemView[T]): tuple[key: int, val: T] {.inline.} =
  ## Iterate over all (index, item) pairs of a view.
  for i in 0 ..< s.len:
    yield (i, s.data[i])

iterator mpairs*[T](s: MemView[T]): tuple[key: int, val: var T] {.inline.} =
  ## Iterate over all (index, item) pairs of a view.
  for i in 0 ..< s.len:
    yield (i, s.data[i])

proc `==`*[T](a, b: MemView[T]): bool {.inline.} =
  ## Compare two views for equality. Two views are equal iff they have the
  ## same lenght and for every position ``i`` on both views it's elements
  ## are equal.
  if a.len != b.len: return false
  for i in 0 ..< a.len:
    if a[i] != b[i]: return false
  return true

template sliceBoundsCheck(data, bounds) =
  when compileOption("boundChecks"):  # d:release disables this.
    if data^^bounds.a < 0 or (0 + data^^bounds.b) >= data.len:
      raise newException(IndexError, "Out of bounds: asked for [" &
                          $(data^^bounds.a) & ", " & $(data^^bounds.b) &
                          "] from [0, " & $data.len & "].")
  discard

template viewImpl() =
  sliceBoundsCheck(data, bounds)
  result.data = unsafeAddr(data[data^^bounds.a])
  result.len = data^^bounds.b - data^^bounds.a + 1

proc view*[T; A, B: SomeIndex](data: seq[T], bounds: HSlice[A, B]): MemView[T] {.inline.} =
  ## Creates a view for a part of an seq. It is the equivalent of an slice
  ## but no copies are made.
  ## If the compile option ``boundsCheck`` is on, it checks if the bounds are
  ## whichin the seq bounds here. Views can't be larger than the original
  ## data.
  viewImpl()

proc view*[A, B: SomeIndex](data: string, bounds: HSlice[A, B]): MemView[char] {.inline.} =
  ## Creates a view for a part of an string. It is the equivalent of an slice
  ## but no copies are made. Also, there is no guarantee that the view will end
  ## in a `\0`.
  ## If the compile option "boundsCheck" is on, it checks if the bounds are
  ## whichin the string bounds here. Views can't be larger than the original
  ## data.
  viewImpl()

proc view*[T](data: ptr T; len: Positive): MemView[T] {.inline.} =
  ## Constructs a view from a pointer to the first element of a contiguous
  ## memory region and a lenght. Can be used with C pointers for
  ## example.
  ## If you want a slice of the original data, either adjust the
  ## pointer to the starting place or call again view() on the output of
  ## this view().
  result.data = data
  result.len = len

proc view*[T; A, B: SomeIndex](data: MemView[T], bounds: HSlice[A, B]): MemView[T] {.inline.} =
  ## Creates a view for a part of an array, seq or another view. It is the
  ## equivalent of an slice but no copies are made.
  ## If the compile option ``boundsCheck`` is on, it checks if the bounds are
  ## whichin the original MemView bounds here. Views can't be larger than the
  ## original data.
  sliceBoundsCheck(data, bounds)
  result.data = data.data + data^^bounds.a  # pointer arithmetic
  result.len = data^^bounds.b - data^^bounds.a + 1

template view*(data: string | MemView | seq): untyped =
  ## Create a view of the entire seq/string.
  ## It is the equivalent of:
  ## data.view(0 .. data.high)
  data.view(0 ..< data.len)

proc viewAs*[T](origData: MemView[T], destType: typedesc): MemView[destType] {.inline.} =
  result.len = (origData.len * T.sizeof) div destType.sizeof
  result.data = cast[ptr destType](origData.dataPtr)

proc viewAs*[T](origData: seq[T], destType: typedesc): MemView[destType] {.inline.} =
  result.len = (origData.len * T.sizeof) div destType.sizeof
  result.data = cast[ptr destType](unsafeaddr(origData[0]))

when false:
  {.push boundChecks:off}
  let bounds: HSlice[A, B] = origData.low .. ((origData.len * T.sizeof) div destType.sizeof)-1
  var data = cast[seq[destType]](origData)
  assert unsafeaddr(data[0]) == unsafeaddr(origData[0])
  viewImpl()
  {.pop.}

proc `[]`*[T; A, B: SomeIndex](data: MemView[T], bounds: HSlice[A, B]): MemView[T] {.inline.} =
  ## Same as view().
  data.view(bounds)

## A couple of pretty unsafe convenient procs. 
## They may be deprecated at any time.

proc dataPtr*[T](s: MemView[T]): ptr T {.inline.} =
  ## Retrive a pointer to the first element of the data that the view is
  ## accessing.
  result = s.data

proc advance*[T](s: var MemView[T], x: int = 1) {.inline.} =
  ## Advance the base of the seq by ``x`` positions. ``x`` can be negative.
  ## Equivalent to s[x .. ^1] but in-place, unsafe and more efficient.
  ## The lenght of the memview can become 0 or negative after calling this proc.
  s.data += x  # pointer arithmetic
  s.len -= x
  #assert s.len > 0

proc setLen*[T](s: var MemView[T], newLen: int) {.inline.}=
  ## Change the len of a view. Equivalent to using `len=` and equaly unsafe.
  ## Not sure if it's a good idea.
  s.len = newLen

proc pop*[T](v: var MemView[T]): T {.inline.} =
  ## Unsafe! Returns the last element and decrements the lenght of `v`.
  ## The calling code is responsible to ensure there is at least one element
  ## left before calling this proc.
  result = v[^1]
  dec v.len

proc top*[T](v: MemView[T]): var T {.inline.} =
  ## Pure alias to v[^1] 
  v[^1]
  
proc add*[T](v: var MemView[T], val: T) {.inline.} =
  ## Unsafe! Adds an element to a memview, incrementing it's length.
  ## Unlike with seqs, strings and other native nim types, it will never resize
  ## the underlying data array and, as MemView don't have a capacity field, it 
  ## can't make a meaningful bounds checks either. Both of those things are 
  ## responsability of the calling code.
  v[+++v.len] = val


## Convenience converters.

#converter toMemView*[T](data: var seq[T]): MemView[T] =
#  data.view(0 .. data.high)

#converter toMemView*(data: var string): MemView[char] =
#  data.view(0 .. data.high)


proc allocMemView*(typ: typedesc, len: Natural): MemView[typ] =
  ## Allocs memory via alloc() and returns in the form of a MemView.
  ##
  ## Will not be freed automatically by the GC, so it requires a call to
  ## dealloc(mview.dataPtr) after use.
  view(cast[ptr typ](alloc(typ.sizeof * len)), len)

proc alloc0MemView*(typ: typedesc, len: Natural): MemView[typ] =
  ## Allocs memory via alloc0() and returns in the form of a MemView.
  ##
  ## Will not be freed automatically by the GC, so it requires a call to
  ## dealloc(mview.dataPtr) after use. 
  view(cast[ptr typ](alloc0(typ.sizeof * len)), len)

proc `$`*[T](s: MemView[T]): string =
  ## Gives a string representation of a view as if it was an array.
  if s.len == 0:
    return "[]"
  result = "[" & $s[0]
  for i in 1 .. s.len - 1:
    result &= ", " & $s[i]
  result &= "]"

proc toSeq*[T](s: Memview[T]): seq[T] =
  ## Shallow copy the content from the memview to a new seq.
  result = newSeq[T](s.len)
  for i, e in s:
    shallowCopy(result[i], e)  # TODO: optimize for types w/o refs

proc toString*(s:MemView[char]): string =
  ## Copy the content from the memview to a new string.
  result = newString(s.len)
  for i, e in s:
    result[i] = e

## Barebones serialization support:

proc storeToFile*[T](v: MemView[T], fname: string) =
  ## Stores sequence of raw bytes of a MemView to a file, with the
  ## native endianess and no special metadata.
  var f = open(fname, fmWrite)
  defer: f.close()
  let size = v.len * T.sizeof 
  if f.writeBuffer(v.dataPtr, size) != size:
    raise newException(IOError, "Couldn't complete write to " & fname)

proc loadFromFile*[T](dest: var MemView[T], fname:string): int64 =
  ## Load raw bytes from file and cast the result to the `dest` type. 
  ##
  ## `dest` lenght must be enough to hold the entire file content. The lenght
  ## will be shortened to equal the file content's lenght and the return value
  ## will be 0. Otherwise, this proc will not do anything and return the 
  ## required lenght.
  var f = open(fname)
  defer: f.close()
  let fsize = f.getFileSize
  let dsize = fsize div T.sizeof
  assert fsize mod T.sizeof == 0
  if dsize > dest.len:
    return dsize
  dest.len = dsize.Natural
  if f.readBuffer(dest.dataPtr, fsize) != fsize:
    raise newException(IOError, "Couldn't complete read from " & fname)

proc loadFromFile*[T](dest: var seq[T], fname:string) =
  ## Load raw bytes from file and cast the result to the `dest` type. 
  ## `dest` will be automatically resized to hold the data.
  var f = open(fname)
  defer: f.close()
  let fsize = f.getFileSize
  let dsize = fsize div T.sizeof
  assert fsize mod T.sizeof == 0
  dest.setLen(dsize)
  if f.readBuffer(addr dest[0], fsize) != fsize:
    raise newException(IOError, "Couldn't complete read from " & fname)

## Example procs using memviews

iterator shallowSplit*(s: MemView[char], seps: set[char] = Whitespace): MemView[char] =
  var last = 0
  while last < s.len:
    while s[last] in seps:
      inc last
    var first = last
    while last < s.len and s[last] notin seps:
      inc last
    if first <= last - 1:
      yield s[first ..< last]

iterator shallowSplit*[T](s: MemView[T], sep: T ): MemView[T] =
  if s.len > 0:
    cfor (var last = 0), last <= s.len, inc last:
      var first = last
      while last < s.len and s[last] != sep:
        inc last
      yield s[first, last-1]

iterator shallowSplit*[T](s: MemView[T], sep: MemView[T] ): MemView[T] =
  if s.len > 0:
    cfor (var last = 0), last <= s.len, last += sep.len:
      var first = last
      while last < s.len and s[last ..< sep.len] != sep:
        inc last
      yield s[first, last-1]


when isMainModule:
  template acesses(test) =
    echo "test: ", test
    echo test[0]
    echo test[2]
    echo "last:", test[^1], " space ", test[test.len - 1]

  proc testAcceptance(s: MemView) =
    echo s[1]

  var s = @[1,2,3,4,5,6,7,8,9]
  #import times
  #s.add(int times.getTime()) # trying to force it to be allocated on the heap
  #echo s
  echo repr s
  echo "Now starts the test:"
  var test = s.view
  #GC_fullCollect()
  acesses(test)

  var test2 = test.view(0 .. 3)
  acesses(test2)

  for x in test2.mitems:
    x = 0

  echo test
  #testAcceptance(s)

  var str = ""
  str &= "abcdef"
  var teststr = str.view(1 .. 4)
  GC_fullCollect()
  teststr.acesses()
  #testAcceptance(teststr)
