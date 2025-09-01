<?php

$buffer      = trim(fgets(STDIN));
$command_cnt = (int)trim(fgets(STDIN));
$cursor      = strlen($buffer);

while ($command_cnt > 0) {
  $tokens  = explode(" ", fgets(STDIN));
  $command = trim($tokens[0]);
  $value   = $tokens[1] ?? 0;
  $value   = trim($value);

  match ($command) {
    "L" => $cursor >= 1 and $cursor--,
    "D" => $cursor < strlen($buffer) and $cursor++,
    "B" => $cursor >= 1 and del($buffer, --$cursor),
    "P" => add($buffer, $cursor++, $value)
  };
  $command_cnt -= 1;
}

printf("%s%s", $buffer, PHP_EOL);

function add(string &$buffer, int $pos, string $value): void {
  $new_buffer = "";

  $buff1 = substr($buffer, 0, $pos);
  $buff2 = substr($buffer, $pos);
  $buffer = $buff1 . $value . $buff2;
}

function del(string &$buffer, int $pos): void {
  if (!isset($buffer[$pos])) {
    return;
  }

  $buff1 = substr($buffer, 0, $pos);
  $buff2 = substr($buffer, $pos + 1);
  $buffer = $buff1 . $buff2;
}
