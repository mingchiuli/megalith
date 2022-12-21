package com.chiu.megalith.common.jwt;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.JwtException;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.security.Keys;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import java.util.Date;
import java.util.Optional;

/**
 * jwt工具类
 */
@Slf4j
@Data
@Component
@ConfigurationProperties(prefix = "blog.jwt")
public class JwtUtils {

    private String secret;

    private long expire;

    /**
     * 生成jwt token
     */
    public String generateToken(String userId, String role) {
        Date nowDate = new Date();
        //过期时间
        Date expireDate = new Date(nowDate.getTime() + expire * 1000);

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
        try {
            return Optional.ofNullable(
                    Jwts.
                            parserBuilder().
                            setSigningKey(Keys.hmacShaKeyFor(secret.getBytes())).
                            build().
                            parseClaimsJws(token).
                            getBody());
        } catch (JwtException e){
            log.debug("validate is token error ", e);
            return Optional.empty();
        }
    }

    /**
     * token是否过期
     * @return  true：过期
     */
    public boolean isTokenExpired(Date expiration) {
        return expiration.before(new Date());
    }
}
