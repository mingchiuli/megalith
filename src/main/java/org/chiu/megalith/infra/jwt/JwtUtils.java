package org.chiu.megalith.infra.jwt;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.security.Keys;
import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import java.util.Date;
import java.util.Optional;

/**
 * jwt工具类
 */
@Data
@Component
@ConfigurationProperties(prefix = "blog.jwt")
public class JwtUtils {

    private String secret;


    public String generateToken(String userId, String role, long expire) {
        var nowDate = new Date();
        //过期时间
        var expireDate = new Date(nowDate.getTime() + expire * 1000);

        return Jwts.builder()
                .setHeaderParam("typ", "JWT")
                .claim("role", role)
                .setSubject(userId)
                .setIssuedAt(nowDate)
                .setExpiration(expireDate)
                .signWith(Keys.hmacShaKeyFor(secret.getBytes()) ,SignatureAlgorithm.HS512)
                .compact();
    }

    public Optional<Claims> getClaimByToken(String token) {
        return Optional.ofNullable(Jwts.parserBuilder()
                .setSigningKey(Keys.hmacShaKeyFor(secret.getBytes()))
                .build()
                .parseClaimsJws(token)
                .getBody()
                );
    }

    public boolean isTokenExpired(Date expiration) {
        return expiration.before(new Date());
    }
}
