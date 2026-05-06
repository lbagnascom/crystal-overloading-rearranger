@[Slot]
def foo(x : Bool, y)
  "x : Int32, y"
end

@[Slot]
def foo(x, y : Bool)
  "x, y : Int32"
end

pp foo(true, true)
