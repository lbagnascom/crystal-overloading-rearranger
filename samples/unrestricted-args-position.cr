@[Slot]
def foo(x : Bool, y)
  "x : Bool, y"
end

@[Slot]
def foo(x, y : Bool)
  "x, y : Bool"
end

pp foo(true, true)
