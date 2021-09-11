
(in-package #:lsa)

(stringhere:enable-txt-syntax)	  
 
(defun hostapd (iface ssid passphrase &key (channel 0) dsss-cck-40 (hw-mode
								    "a"
								    ))
  (declare (type (string iface)))
  (let ((output
	 (with-output-to-string (*standard-output*)
#{
### FILE: hostapd.conf
### Wireless network name ###                                                                                                                          

interface=,(princ iface)
driver=nl80211
country_code=US
ssid=,(princ ssid)
hw_mode=,(princ hw-mode)

## gus - added 2020-03-01
ieee80211n=1
,(if (eq hw-mode #\a) (princ "ieee80211n=1          # 802.11n support"))
ieee80211d=1          # limit the frequencies used to those allowed in the country
wmm_enabled=1
ht_capab=[HT40][SHORT-GI-20],(when dsss-cck-40 (princ "[DSSS_CCK-40]"))
ignore_broadcast_ssid=0

channel=,(princ channel)
wpa=2
wpa_passphrase=,(princ passphrase)
## Key management algorithms ##                                                                                                                        
wpa_key_mgmt=WPA-PSK

## Set cipher suites (encryption algorithms) ##                                                                                                        
## TKIP = Temporal Key Integrity Protocol                                                                                                              
## CCMP = AES in Counter mode with CBC-MAC                                                                                                             
wpa_pairwise=TKIP
rsn_pairwise=CCMP

## Shared Key Authentication ##                                                                                                                        
auth_algs=1

## Accept all MAC address ###                                                                                                                          
macaddr_acl=0
}
)))
    output)
  )


(stringhere:disable-txt-syntax)

