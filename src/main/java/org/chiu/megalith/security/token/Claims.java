package org.chiu.megalith.security.token;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class Claims {

  //userId
  private String sub;

  private String role;
}
