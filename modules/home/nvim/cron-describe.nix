{ pkgs }:
pkgs.writeScriptBin "cron-describe" # python
  ''
    #!${pkgs.python3.withPackages (p: [ p.cron-descriptor ])}/bin/python3
    from cron_descriptor import get_description
    import sys
    if len(sys.argv) != 2:
        print("Usage: cron-describe '<cron_expression>'")
        sys.exit(1)
    print(get_description(sys.argv[1]))
  ''
