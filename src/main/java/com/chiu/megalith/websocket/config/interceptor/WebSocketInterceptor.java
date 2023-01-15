package com.chiu.megalith.websocket.config.interceptor;

import com.chiu.megalith.common.exception.AuthenticationExceptionImpl;
import com.chiu.megalith.common.jwt.JwtUtils;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.JwtException;
import lombok.RequiredArgsConstructor;
import org.apache.http.HttpHeaders;
import org.springframework.lang.NonNull;
import org.springframework.messaging.Message;
import org.springframework.messaging.MessageChannel;
import org.springframework.messaging.simp.stomp.StompCommand;
import org.springframework.messaging.simp.stomp.StompHeaderAccessor;
import org.springframework.messaging.support.ChannelInterceptor;
import org.springframework.messaging.support.MessageHeaderAccessor;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.web.authentication.preauth.PreAuthenticatedAuthenticationToken;
import org.springframework.stereotype.Component;

import java.util.*;

/**
 * @author mingchiuli
 * @create 2022-06-17 9:46 PM
 */
@Component
@RequiredArgsConstructor
public class WebSocketInterceptor implements ChannelInterceptor {

    private final JwtUtils jwtUtils;

//    private final DefaultRoleHolder defaultRoleHolder;
//
//    private final HighestRoleHolder highestRoleHolder;

//    private final List<String> roles = new ArrayList<>();
//
//
//    @PostConstruct
//    private void init() {
//        roles.addAll(Arrays.stream(defaultRoleHolder.getRole()).toList());
//        roles.add(highestRoleHolder.getRole());
//    }

    @Override
    public Message<?> preSend(@NonNull Message<?> message, @NonNull MessageChannel channel) {
        StompHeaderAccessor stompHeaderAccessor = MessageHeaderAccessor.getAccessor(message, StompHeaderAccessor.class);

        Optional.ofNullable(stompHeaderAccessor).ifPresentOrElse(accessor -> {
            if (StompCommand.CONNECT.equals(accessor.getCommand())) {

//                String type = accessor.getFirstNativeHeader("Type");
//                if (!"Coop".equals(type)) {
//                    return;
//                }

                String token = accessor.getFirstNativeHeader(HttpHeaders.AUTHORIZATION);
                Claims claim = jwtUtils.getClaimByToken(token).
                        orElseThrow(() -> new JwtException("token invalid"));

                if (jwtUtils.isTokenExpired(claim.getExpiration())) {
                    throw new JwtException("token expired");
                }

                String userId = claim.getSubject();
                String role = (String) claim.get("role");

//                if (!roles.contains(role)) {
//                    throw new AuthenticationExceptionImpl("non permit");
//                }

                accessor.setUser(new PreAuthenticatedAuthenticationToken(userId,
                        null,
                        AuthorityUtils.createAuthorityList(role)));
            }
        }, () -> {
            throw new AuthenticationExceptionImpl("please reconnect");
        });

        return message;
    }
}
