
# hs-amplitude


Basic Amplitude client in haskell




example `curl` command showing how to submit data to amplitude:
``` sh
curl -X POST https://api2.amplitude.com/2/httpapi   -H 'Content-Type: application/json' \
  -H 'Accept: */*'   --data @- <<EOF
{
      "api_key": "$BITNOMIAL_AMPLITUDE_API_KEY",
      "events": [{
        "device_id": "readme_testing_testing_device_id-$RANDOM",
        "event_type": "Sign up"
      }]
 }
EOF
```


