package org.chiu.megalith.security.token;

public interface TokenUtils<T> {

    String generateToken(String userId, String role, long expire);

    T getVerifierByToken(String token);
}
