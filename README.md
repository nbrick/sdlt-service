# sdlt-service

Build, test and run:
```sh
stack build
stack test
stack exec sdlt-service-exe  # Starts a server on port 8000.
```

Use the service, e.g.:
```sh
curl localhost:8000/api/stampDutyCalculator -d '{"propertyValue":1000000.0}'
```
