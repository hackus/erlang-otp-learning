🎯 Summary

✔ Two environments:

| Mode      | Command                                    | Purpose                                  |
|-----------|--------------------------------------------|------------------------------------------|
| **CI/CD** | `./docker/scripts/run_tests_wsl.sh --cicd` | Build complete image → run tests     |
| **DEV**   | `./docker/scripts/run_tests_wsl.sh --dev`  | Live code (volumes) → instant tests  |
