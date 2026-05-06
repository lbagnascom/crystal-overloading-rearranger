@[Slot]
def bar()
  "bar()"
end

@[Slot]
def bar(x = true)
  "bar(x = true)"
end

pp bar(false)
