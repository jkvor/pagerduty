{application, pagerduty, [
    {description, "Erlang pagerduty client"},
    {vsn, "1.0"},
    {modules, [
      pagerduty,
      pagerduty_app
    ]},
    {applications, [stdlib, kernel, sasl, inets, crypto, public_key, ssl]},
    {registered, []},
    {mod, {pagerduty_app, []}}
]}.
