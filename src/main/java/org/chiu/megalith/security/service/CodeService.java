package org.chiu.megalith.security.service;

/**
 * @author mingchiuli
 * @create 2022-11-27 8:27 pm
 */
public interface CodeService {

    boolean createEmailCode(String loginName);

    boolean createSMSCode(String loginSMS);
}
