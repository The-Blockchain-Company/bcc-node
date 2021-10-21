To add in node-services.nix

               TraceOptionSeverity  = [
                 {ns = ""; severity = "InfoF";}
                 {ns = "Bcc.Node.AcceptPolicy"; severity = "SilenceF";}
                 {ns = "Bcc.Node.ChainDB"; severity = "DebugF";}
               ];

                TraceOptionDetail = [
                  {ns = ""; detail = "DNormal";}
                  {ns = "Bcc.Node.BlockFetchClient"; detail = "DMinimal";}
               ];

               TraceOptionBackend = [
                 {ns = ""; backends = ["Stdout HumanFormatColoured"; "Forwarder"; "EKGBackend"];}
                 {ns = "Bcc.Node.ChainDB"; backends = ["Forwarder"];}
               ];
               TraceOptionForwarder = {filePath: "/tmp/forwarder.sock";};
