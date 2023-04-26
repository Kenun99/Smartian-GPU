contract Test {
    mapping(address => uint256) nonces;

    function transferProxy(address _from, address _to, uint256 _value, uint256 _feeSmt,
        uint8 _v,bytes32 _r, bytes32 _s) public  returns (bool){

        // if(balances[_from] < _feeSmt + _value) revert();

        uint256 nonce = nonces[_from];
        bytes32 h = keccak256(_from,_to,_value,_feeSmt,nonce);
        if(_from != ecrecover(h,_v,_r,_s)) revert();

        // if(balances[_to] + _value < balances[_to]
        //     || balances[msg.sender] + _feeSmt < balances[msg.sender]) revert();
        // balances[_to] += _value;
        // Transfer(_from, _to, _value);

        // balances[msg.sender] += _feeSmt;
        // Transfer(_from, msg.sender, _feeSmt);

        // balances[_from] -= _value + _feeSmt;
        nonces[_from] = nonce + 1;
        return true;
    }
}
