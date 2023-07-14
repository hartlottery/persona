# oh-my-fish/jump-plugin
function c
  if test (count $argv) -ne 1
    m
  else if [ "$argv[1]" = "-h" ]
    echo "Usage:"
    echo "  c        show all marks"
    echo "  c <mark> change to mark"
    return 0
  else
    if test -d $MARKPATH/$argv[1] -a -L $MARKPATH/$argv[1]
      cd (readlink $MARKPATH/$argv[1])
    else
      echo "No such mark: $argv[1]"
    end
  end
  return 0
end
