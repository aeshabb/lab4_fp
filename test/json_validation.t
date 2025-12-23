Тестирование валидатора JSON файлов

  $ json_validator json/simple.json
  Valid JSON
  {"name": "Alice", "age": 30, "active": true}

  $ json_validator json/array.json
  Valid JSON
  [1, 2, 3, 4, 5]

  $ json_validator json/nested.json
  Valid JSON
  {"users": [{"name": "Alice", "role": "admin"}, {"name": "Bob", "role": "user"}]}

  $ json_validator json/mixed.json
  Valid JSON
  [null, true, false, 42, "hello"]
