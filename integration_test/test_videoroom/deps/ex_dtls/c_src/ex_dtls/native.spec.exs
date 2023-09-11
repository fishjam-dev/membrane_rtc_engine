module ExDTLS.Native

interface [NIF, CNode]

state_type "State"

spec init(client_mode :: bool, dtls_srtp :: bool) :: {:ok :: label, state}

spec init_from_key_cert(client_mode :: bool, dtls_srtp :: bool, pkey :: payload, cert :: payload) :: {:ok :: label, state}

spec generate_cert(state) :: {{:ok :: label, cert :: payload}, state}

spec get_pkey(state) :: {{:ok :: label, pkey :: payload}, state}

spec get_cert(state) :: {{:ok :: label, cert :: payload}, state}

spec get_cert_fingerprint(state) :: {{:ok :: label, fingerprint :: payload}, state}

spec do_handshake(state) :: {{:ok :: label, packets :: payload}, state}

spec handle_timeout(state) :: {:ok :: label, state}
                           | {{:retransmit :: label, packets :: payload}, state}

spec process(state, packets :: payload) :: {:ok :: label, state, packets :: payload}
                                           | {(:hsk_want_read :: label), state}
                                           | {{:hsk_packets :: label, packets :: payload}, state}
                                           | {{:hsk_finished :: label,
                                              client_keying_material :: payload,
                                              server_keying_material :: payload,
                                              protection_profile :: int,
                                              packets :: payload}, state}
                                           | {{:connection_closed :: label, :peer_closed_for_writing :: label}, state}
