class Foo
  @[Slot]
  def bar()
  end

  @[Slot]
  def bar(a = 0)
  end
end

Foo.new.bar 1
