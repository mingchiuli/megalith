package com.chiu.megalith.security.service;

/**
 * @author mingchiuli
 * @create 2022-11-27 8:27 pm
 */
public interface CodeService {

    Boolean createEmailCode(String loginName);
}
