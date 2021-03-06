pragma solidity ^0.6.0;

// SPDX-License-Identifier: ISC

import "@openzeppelin/contracts/token/ERC20/ERC20.sol";
import "@openzeppelin/contracts/access/Ownable.sol";

contract FungibleToken is ERC20, Ownable {
    constructor(uint256 initialSupply) public ERC20("Fungible Token", "FT") {
        _mint(msg.sender, initialSupply);
    }
}