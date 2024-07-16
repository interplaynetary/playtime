(context (playtime) (
     (roles (
            (player-spawner
                (moves (
                        (<move-name> <script>)
                        ➕)
                (requires <role-play-contract>)
                (role-player <element>))
            (element-adder
                (moves (
                        (<move-name> <script>)
                        ➕)
                (requires <role-play-contract>)
                (role-player <element>))
            (scope-recognizer
                (moves (
                        (<move-name> <script>)
                        ➕)
                (requires <role-play-contract>)
                (role-player <element>))
            (scope-recognition-recognizer
                (moves (
                        (<move-name> <script>)
                        ➕)
                (requires <role-play-contract>)
                (role-player <element>))
            (pattern-type-recognizer
                (moves (
                        (<move-name> <script>)
                        ➕)
                (requires <role-play-contract>)
                (role-player <element>))
            (pattern-type-instantiator
                (moves (
                        (<move-name> <script>)
                        ➕)
                (requires <role-play-contract>)
                (role-player <element>))
            (perspective-view-shifter
                (moves (
                        (<move-name> <script>)
                        ➕)
                (requires <role-play-contract>)
                (role-player <element>))
            (commenter
                (moves (
                        (<move-name> <script>)
                        ➕)
                (requires <role-play-contract>)
                (role-player <element>))
            (play-reader
                (moves (
                        (<move-name> <script>)
                        ➕)
                (requires <role-play-contract>)
                (role-player <element>))
            (play-writer
                (moves (
                        (<move-name> <script>)
                        ➕)
                (requires <role-play-contract>)
                (role-player
                    (context (play-writer-interface) (
     			        (roles (
           				    (role-caster
                  				(moves (
                        			(<move-name> <script>)
                    				➕)
                                (requires <role-play-contract>)
                  				(role-player 
                                    (context (role-caster-interface) (
     					                (roles (
			    		                	(role-offerer
      						            		(moves (
        						            	     (<move-name> <script>)
                                               				➕)
                  		       	        	    (requires <role-play-contract>)
                                                (role-player <element>))
 			       			                (role-acceptor
                					            (moves (
        								 (<move-name> <script>)
                        				            ➕)
                  					            (requires <role-play-contract>)
                                        			    (role-player <element>))
            				                ➕))
    					                (enactment <script>)))))
                                (role-play-contractor
                                    (moves (
                                        (<move-name> <script>)
                                        ➕)
                                    (requires <role-play-contract>)
                                    (role-player <element>))
                                ➕))
    	                        (enactment <script>)))))
            (play-enacter
                  (moves (
                        (<move-name> <script>)
                        ➕)
                  (requires <role-play-contract>)
                  (role-player <element>))
            (play-performance
                  (moves (
                        (<move-name> <script>)
                        ➕)
                  (requires <role-play-contract>)
                  (role-player <element>))
            ➕))
    (enactment <script>)))
