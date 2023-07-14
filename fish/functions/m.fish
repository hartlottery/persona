# oh-my-fish/plugin-jump
function m
  if test (count $argv) -eq 0
    set -l file_list (command ls $MARKPATH)
    if test (count $file_list) -eq 0
      echo "No marks currently defined."
    else
      set -l mark_list
      for file in $file_list
        if test -d $MARKPATH/$file -a -L $MARKPATH/$file
          set mark_list $mark_list $file
        end
      end
      if test (count $mark_list) -eq 0
        echo "No marks currently defined."
      else
        set -l output ""
        for mark_name in $mark_list
          set -l real_path (readlink $MARKPATH/$mark_name)
          set output "$output$mark_name -> $real_path"\n
        end
        echo $output | column -t
      end
    end
    return 0
  else if [ "$argv[1]" = "-h" ]
    echo "Usage:"
    echo "  m -h           show this help"
    echo "  m              show all marks"
    echo "  m <mark> [dir] add a mark to dir"
    echo "  m . [dir]      add a numeric mark"
    echo "  m -d <mark...> delete marks"
    echo "  c <mark>       change dir to mark"
    return 0
  else if [ "$argv[1]" = "-d" ]
    for mark_name in $argv[2..-1]
      if test -d $MARKPATH/$mark_name -a -L $MARKPATH/$mark_name
        command rm -i $MARKPATH/$mark_name
      else
        echo "No such mark: $mark_name"
      end
    end
    return 0
  end

  # get mark
  if [ "$argv[1]" = "." ]
    # https://stackoverflow.com/a/15867729
    set -f mark (ls $MARKPATH | grep -E '^[0-9]+$' | sort -n | awk 'BEGIN{p=-1} $1!=p+1{exit}{p=p+1} END{print p+1}')
    if [ "$mark" = "" ]
      echo "ERRMARK: empty"
      return 1
    end
  else if test -e $MARKPATH/$argv[1]
    if test -d $MARKPATH/$argv[1] -a -L $MARKPATH/$argv[1]
      echo "A mark named $argv[1] already exists."
    else
      echo "$MARKPATH/$argv[1] already exists."
    end
    return 1
  else
    set -f mark $argv[1]
  end

  # start marking
  if test (count $argv) -eq 1
    command ln -sv (pwd) $MARKPATH/$mark
  else if test (count $argv) -eq 2
    if test -d $argv[2]
      command ln -sv (realpath $argv[2]) $MARKPATH/$mark
    else
      echo "$argv[2] is not a valid directory."
    end
  end
  return 0
end
