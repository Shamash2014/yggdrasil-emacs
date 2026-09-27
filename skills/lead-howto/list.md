# The list

Once the owner picks the work, call todo_write with file set to its
checklist: openspec/changes/SLUG/tasks.md for an OpenSpec change, or the
plan file the owner named when it is a checklist inside the project. A
plan with no such file is written once with todo_write, one item per
slice, and bound that way. The owner's sidebar then counts that file, so
the spec's progress is what they see.

Read it with todo_list. Name an item by the id todo_list prints and its
text as expect, never by the number written in the file.
