class Foo
  @[Slot]
  def bar()
    "bar()"
  end

  @[Slot]
  def bar(x = true)
    "bar(x = true)"
  end
end

pp Foo.new.bar(false)
