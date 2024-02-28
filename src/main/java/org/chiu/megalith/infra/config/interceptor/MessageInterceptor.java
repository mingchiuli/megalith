package org.chiu.megalith.infra.config.interceptor;

import com.auth0.jwt.exceptions.JWTVerificationException;
import com.auth0.jwt.interfaces.DecodedJWT;
import lombok.RequiredArgsConstructor;

import java.util.Objects;
import org.apache.http.HttpHeaders;
import org.chiu.megalith.infra.token.TokenUtils;
import org.chiu.megalith.security.utils.SecurityAuthenticationUtils;
import org.springframework.lang.NonNull;
import org.springframework.messaging.Message;
import org.springframework.messaging.MessageChannel;
import org.springframework.messaging.simp.stomp.StompCommand;
import org.springframework.messaging.simp.stomp.StompHeaderAccessor;
import org.springframework.messaging.support.ChannelInterceptor;
import org.springframework.messaging.support.MessageHeaderAccessor;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import static org.chiu.megalith.infra.lang.Const.TOKEN_PREFIX;
import static org.chiu.megalith.infra.lang.ExceptionMessage.ACCESSOR_NULL;
import static org.chiu.megalith.infra.lang.ExceptionMessage.TOKEN_INVALID;

/**
 * @author mingchiuli
 * @create 2022-06-17 9:46 PM
 */
@Component
@RequiredArgsConstructor
public class MessageInterceptor implements ChannelInterceptor {

    private final TokenUtils<DecodedJWT> tokenUtils;

    private final SecurityAuthenticationUtils securityAuthenticationUtils;

    @Override
    public Message<?> preSend(@NonNull Message<?> message, @NonNull MessageChannel channel) {
        StompHeaderAccessor accessor = MessageHeaderAccessor.getAccessor(message, StompHeaderAccessor.class);
        Assert.isTrue(Objects.nonNull(accessor), ACCESSOR_NULL.getMsg());
        if (StompCommand.CONNECT.equals(accessor.getCommand())) {
            String token = accessor.getFirstNativeHeader(HttpHeaders.AUTHORIZATION);
            if (StringUtils.hasLength(token)) {
                String jwt;
                try {
                    jwt = token.substring(TOKEN_PREFIX.getInfo().length());
                } catch (IndexOutOfBoundsException e) {
                    throw new JWTVerificationException(TOKEN_INVALID.getMsg());
                }

                DecodedJWT decodedJWT = tokenUtils.getVerifierByToken(jwt);
                String userId = decodedJWT.getSubject();
                String role = decodedJWT.getClaim("role").asString();

                Authentication authentication = securityAuthenticationUtils.getAuthentication(role, userId);
                accessor.setUser(authentication);
            }

        }

        return message;
    }
}
