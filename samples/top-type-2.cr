@[Slot]
def foo(x)
  "x"
end

@[Slot]
def foo(x : _)
  "x : _"
end

pp foo(true)
