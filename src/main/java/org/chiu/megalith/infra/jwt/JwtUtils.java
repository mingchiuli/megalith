package org.chiu.megalith.infra.jwt;

import com.auth0.jwt.JWT;
import com.auth0.jwt.algorithms.Algorithm;
import com.auth0.jwt.interfaces.DecodedJWT;
import com.auth0.jwt.interfaces.JWTVerifier;
import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.Date;

/**
 * jwt工具类
 */
@Data
@Component
@ConfigurationProperties(prefix = "blog.jwt")
public class JwtUtils {

    private String secret;

    private final Algorithm algorithm = Algorithm.HMAC512(secret);

    private final JWTVerifier verifier = JWT.require(algorithm)
            // specify an specific claim validations
            .withIssuer("megalith")
            // reusable verifier instance
            .build();

    public String generateToken(String userId, String role, long expire) {

        var nowDate = new Date();
        //过期时间
        var expireDate = new Date(nowDate.getTime() + expire * 1000);
        return JWT.create()
                .withHeader(Collections.singletonMap("typ", "JWT"))
                .withExpiresAt(expireDate)
                .withSubject(userId)
                .withClaim("role", role)
                .withIssuedAt(nowDate)
                .withIssuer("megalith")
                .sign(algorithm);
    }

    public DecodedJWT getJWTVerifierByToken(String token) {
        return verifier.verify(token);
    }
}
